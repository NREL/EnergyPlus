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

// Example/test case on multlayer specular
// Difference from BSDF layer is that properties can be calculated at any custom angle

class EquivalentSpecularLayer_102_103 : public testing::Test {

private:
	std::shared_ptr< CMultiPaneSpecular > m_Layer;

	std::shared_ptr< CSeries > loadSolarRadiationFile() {

		std::shared_ptr< CSeries > aSolarRadiation = make_shared< CSeries >();

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
		std::shared_ptr< CSpectralSampleData > aMeasurements_102 = make_shared< CSpectralSampleData >();

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

	std::shared_ptr< CSpectralSampleData > loadSampleData_NFRC_103() {
		std::shared_ptr< CSpectralSampleData > aMeasurements_103 = make_shared< CSpectralSampleData >();
		aMeasurements_103->addRecord( 0.300, 0.0000, 0.0470, 0.0490 );
		aMeasurements_103->addRecord( 0.305, 0.0050, 0.0470, 0.0490 );
		aMeasurements_103->addRecord( 0.310, 0.0000, 0.0470, 0.0480 );
		aMeasurements_103->addRecord( 0.315, 0.0030, 0.0460, 0.0480 );
		aMeasurements_103->addRecord( 0.320, 0.0190, 0.0460, 0.0480 );
		aMeasurements_103->addRecord( 0.325, 0.0660, 0.0450, 0.0460 );
		aMeasurements_103->addRecord( 0.330, 0.1600, 0.0450, 0.0470 );
		aMeasurements_103->addRecord( 0.335, 0.2940, 0.0490, 0.0500 );
		aMeasurements_103->addRecord( 0.340, 0.4370, 0.0550, 0.0560 );
		aMeasurements_103->addRecord( 0.345, 0.5660, 0.0620, 0.0620 );
		aMeasurements_103->addRecord( 0.350, 0.6710, 0.0690, 0.0690 );
		aMeasurements_103->addRecord( 0.355, 0.7440, 0.0740, 0.0740 );
		aMeasurements_103->addRecord( 0.360, 0.7930, 0.0780, 0.0780 );
		aMeasurements_103->addRecord( 0.365, 0.8220, 0.0800, 0.0800 );
		aMeasurements_103->addRecord( 0.370, 0.8320, 0.0810, 0.0810 );
		aMeasurements_103->addRecord( 0.375, 0.8190, 0.0800, 0.0800 );
		aMeasurements_103->addRecord( 0.380, 0.8090, 0.0790, 0.0790 );
		aMeasurements_103->addRecord( 0.385, 0.8290, 0.0800, 0.0800 );
		aMeasurements_103->addRecord( 0.390, 0.8530, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.395, 0.8680, 0.0830, 0.0830 );
		aMeasurements_103->addRecord( 0.400, 0.8750, 0.0830, 0.0830 );
		aMeasurements_103->addRecord( 0.410, 0.8750, 0.0830, 0.0830 );
		aMeasurements_103->addRecord( 0.420, 0.8730, 0.0830, 0.0830 );
		aMeasurements_103->addRecord( 0.430, 0.8730, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.440, 0.8730, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.450, 0.8800, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.460, 0.8870, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.470, 0.8900, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.480, 0.8920, 0.0830, 0.0830 );
		aMeasurements_103->addRecord( 0.490, 0.8930, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.500, 0.8940, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.510, 0.8950, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.520, 0.8950, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.530, 0.8940, 0.0820, 0.0820 );
		aMeasurements_103->addRecord( 0.540, 0.8930, 0.0810, 0.0810 );
		aMeasurements_103->addRecord( 0.550, 0.8910, 0.0810, 0.0810 );
		aMeasurements_103->addRecord( 0.560, 0.8880, 0.0810, 0.0810 );
		aMeasurements_103->addRecord( 0.570, 0.8840, 0.0800, 0.0800 );
		aMeasurements_103->addRecord( 0.580, 0.8810, 0.0800, 0.0800 );
		aMeasurements_103->addRecord( 0.590, 0.8760, 0.0790, 0.0790 );
		aMeasurements_103->addRecord( 0.600, 0.8710, 0.0790, 0.0790 );
		aMeasurements_103->addRecord( 0.610, 0.8650, 0.0780, 0.0780 );
		aMeasurements_103->addRecord( 0.620, 0.8590, 0.0770, 0.0770 );
		aMeasurements_103->addRecord( 0.630, 0.8530, 0.0770, 0.0770 );
		aMeasurements_103->addRecord( 0.640, 0.8470, 0.0760, 0.0760 );
		aMeasurements_103->addRecord( 0.650, 0.8400, 0.0750, 0.0750 );
		aMeasurements_103->addRecord( 0.660, 0.8330, 0.0750, 0.0750 );
		aMeasurements_103->addRecord( 0.670, 0.8260, 0.0740, 0.0740 );
		aMeasurements_103->addRecord( 0.680, 0.8180, 0.0730, 0.0730 );
		aMeasurements_103->addRecord( 0.690, 0.8100, 0.0730, 0.0730 );
		aMeasurements_103->addRecord( 0.700, 0.8020, 0.0720, 0.0720 );
		aMeasurements_103->addRecord( 0.710, 0.7940, 0.0710, 0.0720 );
		aMeasurements_103->addRecord( 0.720, 0.7860, 0.0710, 0.0710 );
		aMeasurements_103->addRecord( 0.730, 0.7770, 0.0700, 0.0700 );
		aMeasurements_103->addRecord( 0.740, 0.7690, 0.0690, 0.0700 );
		aMeasurements_103->addRecord( 0.750, 0.7610, 0.0690, 0.0690 );
		aMeasurements_103->addRecord( 0.760, 0.7520, 0.0680, 0.0680 );
		aMeasurements_103->addRecord( 0.770, 0.7440, 0.0670, 0.0680 );
		aMeasurements_103->addRecord( 0.780, 0.7360, 0.0670, 0.0670 );
		aMeasurements_103->addRecord( 0.790, 0.7290, 0.0660, 0.0660 );
		aMeasurements_103->addRecord( 0.800, 0.7220, 0.0660, 0.0660 );
		aMeasurements_103->addRecord( 0.810, 0.7150, 0.0650, 0.0660 );
		aMeasurements_103->addRecord( 0.820, 0.7100, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 0.830, 0.7020, 0.0640, 0.0650 );
		aMeasurements_103->addRecord( 0.840, 0.6980, 0.0640, 0.0640 );
		aMeasurements_103->addRecord( 0.850, 0.6900, 0.0630, 0.0640 );
		aMeasurements_103->addRecord( 0.860, 0.6870, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 0.870, 0.6810, 0.0670, 0.0670 );
		aMeasurements_103->addRecord( 0.880, 0.6770, 0.0650, 0.0660 );
		aMeasurements_103->addRecord( 0.890, 0.6730, 0.0660, 0.0660 );
		aMeasurements_103->addRecord( 0.900, 0.6700, 0.0650, 0.0660 );
		aMeasurements_103->addRecord( 0.910, 0.6670, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 0.920, 0.6640, 0.0640, 0.0640 );
		aMeasurements_103->addRecord( 0.930, 0.6600, 0.0630, 0.0630 );
		aMeasurements_103->addRecord( 0.940, 0.6580, 0.0640, 0.0640 );
		aMeasurements_103->addRecord( 0.950, 0.6560, 0.0630, 0.0630 );
		aMeasurements_103->addRecord( 0.960, 0.6540, 0.0610, 0.0610 );
		aMeasurements_103->addRecord( 0.970, 0.6530, 0.0620, 0.0620 );
		aMeasurements_103->addRecord( 0.980, 0.6510, 0.0610, 0.0620 );
		aMeasurements_103->addRecord( 0.990, 0.6490, 0.0610, 0.0620 );
		aMeasurements_103->addRecord( 1.000, 0.6480, 0.0590, 0.0600 );
		aMeasurements_103->addRecord( 1.050, 0.6450, 0.0590, 0.0600 );
		aMeasurements_103->addRecord( 1.100, 0.6450, 0.0580, 0.0590 );
		aMeasurements_103->addRecord( 1.150, 0.6470, 0.0590, 0.0590 );
		aMeasurements_103->addRecord( 1.200, 0.6530, 0.0590, 0.0590 );
		aMeasurements_103->addRecord( 1.250, 0.6610, 0.0580, 0.0590 );
		aMeasurements_103->addRecord( 1.300, 0.6730, 0.0600, 0.0600 );
		aMeasurements_103->addRecord( 1.350, 0.6870, 0.0600, 0.0600 );
		aMeasurements_103->addRecord( 1.400, 0.7020, 0.0610, 0.0610 );
		aMeasurements_103->addRecord( 1.450, 0.7220, 0.0610, 0.0620 );
		aMeasurements_103->addRecord( 1.500, 0.7410, 0.0630, 0.0640 );
		aMeasurements_103->addRecord( 1.550, 0.7570, 0.0630, 0.0640 );
		aMeasurements_103->addRecord( 1.600, 0.7690, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 1.650, 0.7750, 0.0650, 0.0640 );
		aMeasurements_103->addRecord( 1.700, 0.7790, 0.0640, 0.0650 );
		aMeasurements_103->addRecord( 1.750, 0.7790, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 1.800, 0.7770, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 1.850, 0.7760, 0.0650, 0.0630 );
		aMeasurements_103->addRecord( 1.900, 0.7730, 0.0620, 0.0620 );
		aMeasurements_103->addRecord( 1.950, 0.7730, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 2.000, 0.7720, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 2.050, 0.7740, 0.0640, 0.0640 );
		aMeasurements_103->addRecord( 2.100, 0.7750, 0.0640, 0.0650 );
		aMeasurements_103->addRecord( 2.150, 0.7730, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 2.200, 0.7580, 0.0640, 0.0650 );
		aMeasurements_103->addRecord( 2.250, 0.7590, 0.0640, 0.0640 );
		aMeasurements_103->addRecord( 2.300, 0.7660, 0.0650, 0.0650 );
		aMeasurements_103->addRecord( 2.350, 0.7670, 0.0640, 0.0650 );
		aMeasurements_103->addRecord( 2.400, 0.7660, 0.0640, 0.0640 );
		aMeasurements_103->addRecord( 2.450, 0.7570, 0.0640, 0.0640 );
		aMeasurements_103->addRecord( 2.500, 0.7500, 0.0630, 0.0630 );

		return aMeasurements_103;
	}

protected:
	virtual void SetUp() {
		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();

		std::shared_ptr< CSpectralSampleData > aMeasurements_102 = loadSampleData_NFRC_102();
		std::shared_ptr< CSpectralSampleData > aMeasurements_103 = loadSampleData_NFRC_103();

		std::shared_ptr< CSpectralSample > aSample_102 = make_shared< CSpectralSample >( aMeasurements_102 );

		double thickness = 3.048e-3; // [m]
		std::shared_ptr< CMaterialSample > aMaterial_102 = make_shared< CMaterialSample >( aSample_102,
		                                                                              thickness, MaterialType::Monolithic, WavelengthRange::Solar );

		// std::shared_ptr< CSpecularCellDescription > aCellDescription_102 = make_shared< CSpecularCellDescription >();
		std::shared_ptr< CSpecularCell > aCell_102 = make_shared< CSpecularCell >( aMaterial_102 );

		std::shared_ptr< CSpectralSample > aSample_103 = make_shared< CSpectralSample >( aMeasurements_103 );

		thickness = 5.715e-3; // [m]
		std::shared_ptr< CMaterialSample > aMaterial_103 = make_shared< CMaterialSample >( aSample_103,
		                                                                              thickness, MaterialType::Monolithic, WavelengthRange::Solar );

		// std::shared_ptr< CSpecularCellDescription > aCellDescription_103 = make_shared< CSpecularCellDescription >();

		std::shared_ptr< CSpecularCell > aCell_103 = make_shared< CSpecularCell >( aMaterial_103 );

		// To assure interpolation to common wavelengths. MultiBSDF will NOT work with different wavelengths
		CCommonWavelengths aCommonWL;
		aCommonWL.addWavelength( aCell_102->getBandWavelengths() );
		aCommonWL.addWavelength( aCell_103->getBandWavelengths() );

		// Finds combination of two wavelength sets without going outside of wavelenght range for any of spectral samples.
		vector< double > commonWavelengths = aCommonWL.getCombinedWavelengths( Combine::Interpolate );

		m_Layer = make_shared< CMultiPaneSpecular >( commonWavelengths, aSolarRadiation, aCell_102 );
		m_Layer->addLayer( aCell_103 );

	}

public:
	std::shared_ptr< CMultiPaneSpecular > getLayer() {
		return m_Layer;
	};

};

TEST_F( EquivalentSpecularLayer_102_103, TestAngle0 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 0 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 0;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.65230205286826037, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12479902776636984, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.11668348220268998, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.096042310898170405, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12685660846719965, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle10 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 10 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 10;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.65120835620363415, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12470689247907361, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.11655371916284864, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.096618407235493114, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12746634408179885, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle20 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 20 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 20;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.64751806671867440, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12489838684275215, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.11660336773443981, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.098354340958477934, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12922920548009562, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle30 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 30 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 30;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.63972185027772543, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12711434211979922, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.11846025537743926, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.10127416696834185, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.13188964063413330, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle40 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 40 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 40;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.62415746857992227, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.13562252131149963, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12609651562627408, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.10542718735920054, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.13479282274937771, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle50 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 50 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 50;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.59250393302388293, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.16019751863508175, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.14857608587826943, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.11091849816937351, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.13638005017166166, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle60 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 60 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 60;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.52700738355329890, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.22193075739374207, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.20547728739801585, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.11791975306119040, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.13314210599176837, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle70 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 70 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 70;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.39704764366132467, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.35887428072908673, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.33279817614050600, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12600044978532798, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.11807762582426075, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle80 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 80 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 80;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.18476387225635726, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.60689021339883631, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.57045534006477172, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12792157257219708, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.080424341772609279, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngle90 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 90 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 90;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 1, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 1, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngleHemispherical10 ) {
	SCOPED_TRACE( "Begin Test: Hemispherical to hemispherical with ten integration points." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	std::shared_ptr< std::vector< double > > aAngles = make_shared< std::vector< double > >();

	*aAngles = { 0, 10, 20, 30, 40, 50, 60, 70, 80, 90 };

	CMultiPaneSpecular aLayer = *getLayer();

	double Tfhem = aLayer.getHemisphericalProperty( Side::Front, Property::T, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.55256216101095457, Tfhem, 1e-6 );

	double Tbhem = aLayer.getHemisphericalProperty( Side::Back, Property::T, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.55256216101095457, Tbhem, 1e-6 );

	double Rfhem = aLayer.getHemisphericalProperty( Side::Front, Property::R, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.20154919359225856, Rfhem, 1e-6 );

	double Rbhem = aLayer.getHemisphericalProperty( Side::Back, Property::R, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.18760169405134167, Rbhem, 1e-6 );

	double Abs1 = aLayer.AbsHemispherical( 1, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.10889040913346858, Abs1, 1e-6 );

	double Abs2 = aLayer.AbsHemispherical( 2, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.12682364187155989, Abs2, 1e-6 );

}

TEST_F( EquivalentSpecularLayer_102_103, TestAngleHemispherical19 ) {
	SCOPED_TRACE( "Begin Test: Hemispherical to hemispherical with nineteen integration points." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	std::shared_ptr< std::vector< double > > aAngles = make_shared< std::vector< double > >();

	*aAngles = { 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90 };

	CMultiPaneSpecular aLayer = *getLayer();

	double Tfhem = aLayer.getHemisphericalProperty( Side::Front, Property::T, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.55493570125786351, Tfhem, 1e-6 );

	double Tbhem = aLayer.getHemisphericalProperty( Side::Back, Property::T, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.55493570125786351, Tbhem, 1e-6 );

	double Rfhem = aLayer.getHemisphericalProperty( Side::Front, Property::R, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.20564032415202421, Rfhem, 1e-6 );

	double Rbhem = aLayer.getHemisphericalProperty( Side::Back, Property::R, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.19161090008117540, Rbhem, 1e-6 );

	double Abs1 = aLayer.AbsHemispherical( 1, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.10955413074963188, Abs1, 1e-6 );

	double Abs2 = aLayer.AbsHemispherical( 2, aAngles, minLambda, maxLambda );
	EXPECT_NEAR( 0.12733007563220630, Abs2, 1e-6 );

}
