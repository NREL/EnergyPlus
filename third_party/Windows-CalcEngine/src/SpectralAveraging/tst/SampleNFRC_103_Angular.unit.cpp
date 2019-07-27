#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"


using namespace SpectralAveraging;
using namespace FenestrationCommon;

// Example (test case) of sample that calculates angular properties of single layer sample

class TestSampleNFRC_103_Angular : public testing::Test {

private:
	std::shared_ptr< CAngularSpectralSample > m_Sample;

protected:
	std::shared_ptr< CSeries > getSolarRadiation() const {
		auto aSolarRadiation = std::make_shared< CSeries >();

		// Full ASTM E891-87 Table 1
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

	std::shared_ptr< CSpectralSampleData > getMeasurements() const {
		auto aMeasurements = std::make_shared< CSpectralSampleData >();

		aMeasurements->addRecord( 0.300, 0.0000, 0.0470, 0.0490 );
		aMeasurements->addRecord( 0.305, 0.0050, 0.0470, 0.0490 );
		aMeasurements->addRecord( 0.310, 0.0000, 0.0470, 0.0480 );
		aMeasurements->addRecord( 0.315, 0.0030, 0.0460, 0.0480 );
		aMeasurements->addRecord( 0.320, 0.0190, 0.0460, 0.0480 );
		aMeasurements->addRecord( 0.325, 0.0660, 0.0450, 0.0460 );
		aMeasurements->addRecord( 0.330, 0.1600, 0.0450, 0.0470 );
		aMeasurements->addRecord( 0.335, 0.2940, 0.0490, 0.0500 );
		aMeasurements->addRecord( 0.340, 0.4370, 0.0550, 0.0560 );
		aMeasurements->addRecord( 0.345, 0.5660, 0.0620, 0.0620 );
		aMeasurements->addRecord( 0.350, 0.6710, 0.0690, 0.0690 );
		aMeasurements->addRecord( 0.355, 0.7440, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.360, 0.7930, 0.0780, 0.0780 );
		aMeasurements->addRecord( 0.365, 0.8220, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.370, 0.8320, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.375, 0.8190, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.380, 0.8090, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.385, 0.8290, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.390, 0.8530, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.395, 0.8680, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.400, 0.8750, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.410, 0.8750, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.420, 0.8730, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.430, 0.8730, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.440, 0.8730, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.450, 0.8800, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.460, 0.8870, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.470, 0.8900, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.480, 0.8920, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.490, 0.8930, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.500, 0.8940, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.510, 0.8950, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.520, 0.8950, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.530, 0.8940, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.540, 0.8930, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.550, 0.8910, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.560, 0.8880, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.570, 0.8840, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.580, 0.8810, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.590, 0.8760, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.600, 0.8710, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.610, 0.8650, 0.0780, 0.0780 );
		aMeasurements->addRecord( 0.620, 0.8590, 0.0770, 0.0770 );
		aMeasurements->addRecord( 0.630, 0.8530, 0.0770, 0.0770 );
		aMeasurements->addRecord( 0.640, 0.8470, 0.0760, 0.0760 );
		aMeasurements->addRecord( 0.650, 0.8400, 0.0750, 0.0750 );
		aMeasurements->addRecord( 0.660, 0.8330, 0.0750, 0.0750 );
		aMeasurements->addRecord( 0.670, 0.8260, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.680, 0.8180, 0.0730, 0.0730 );
		aMeasurements->addRecord( 0.690, 0.8100, 0.0730, 0.0730 );
		aMeasurements->addRecord( 0.700, 0.8020, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.710, 0.7940, 0.0710, 0.0720 );
		aMeasurements->addRecord( 0.720, 0.7860, 0.0710, 0.0710 );
		aMeasurements->addRecord( 0.730, 0.7770, 0.0700, 0.0700 );
		aMeasurements->addRecord( 0.740, 0.7690, 0.0690, 0.0700 );
		aMeasurements->addRecord( 0.750, 0.7610, 0.0690, 0.0690 );
		aMeasurements->addRecord( 0.760, 0.7520, 0.0680, 0.0680 );
		aMeasurements->addRecord( 0.770, 0.7440, 0.0670, 0.0680 );
		aMeasurements->addRecord( 0.780, 0.7360, 0.0670, 0.0670 );
		aMeasurements->addRecord( 0.790, 0.7290, 0.0660, 0.0660 );
		aMeasurements->addRecord( 0.800, 0.7220, 0.0660, 0.0660 );
		aMeasurements->addRecord( 0.810, 0.7150, 0.0650, 0.0660 );
		aMeasurements->addRecord( 0.820, 0.7100, 0.0650, 0.0650 );
		aMeasurements->addRecord( 0.830, 0.7020, 0.0640, 0.0650 );
		aMeasurements->addRecord( 0.840, 0.6980, 0.0640, 0.0640 );
		aMeasurements->addRecord( 0.850, 0.6900, 0.0630, 0.0640 );
		aMeasurements->addRecord( 0.860, 0.6870, 0.0650, 0.0650 );
		aMeasurements->addRecord( 0.870, 0.6810, 0.0670, 0.0670 );
		aMeasurements->addRecord( 0.880, 0.6770, 0.0650, 0.0660 );
		aMeasurements->addRecord( 0.890, 0.6730, 0.0660, 0.0660 );
		aMeasurements->addRecord( 0.900, 0.6700, 0.0650, 0.0660 );
		aMeasurements->addRecord( 0.910, 0.6670, 0.0650, 0.0650 );
		aMeasurements->addRecord( 0.920, 0.6640, 0.0640, 0.0640 );
		aMeasurements->addRecord( 0.930, 0.6600, 0.0630, 0.0630 );
		aMeasurements->addRecord( 0.940, 0.6580, 0.0640, 0.0640 );
		aMeasurements->addRecord( 0.950, 0.6560, 0.0630, 0.0630 );
		aMeasurements->addRecord( 0.960, 0.6540, 0.0610, 0.0610 );
		aMeasurements->addRecord( 0.970, 0.6530, 0.0620, 0.0620 );
		aMeasurements->addRecord( 0.980, 0.6510, 0.0610, 0.0620 );
		aMeasurements->addRecord( 0.990, 0.6490, 0.0610, 0.0620 );
		aMeasurements->addRecord( 1.000, 0.6480, 0.0590, 0.0600 );
		aMeasurements->addRecord( 1.050, 0.6450, 0.0590, 0.0600 );
		aMeasurements->addRecord( 1.100, 0.6450, 0.0580, 0.0590 );
		aMeasurements->addRecord( 1.150, 0.6470, 0.0590, 0.0590 );
		aMeasurements->addRecord( 1.200, 0.6530, 0.0590, 0.0590 );
		aMeasurements->addRecord( 1.250, 0.6610, 0.0580, 0.0590 );
		aMeasurements->addRecord( 1.300, 0.6730, 0.0600, 0.0600 );
		aMeasurements->addRecord( 1.350, 0.6870, 0.0600, 0.0600 );
		aMeasurements->addRecord( 1.400, 0.7020, 0.0610, 0.0610 );
		aMeasurements->addRecord( 1.450, 0.7220, 0.0610, 0.0620 );
		aMeasurements->addRecord( 1.500, 0.7410, 0.0630, 0.0640 );
		aMeasurements->addRecord( 1.550, 0.7570, 0.0630, 0.0640 );
		aMeasurements->addRecord( 1.600, 0.7690, 0.0650, 0.0650 );
		aMeasurements->addRecord( 1.650, 0.7750, 0.0650, 0.0640 );
		aMeasurements->addRecord( 1.700, 0.7790, 0.0640, 0.0650 );
		aMeasurements->addRecord( 1.750, 0.7790, 0.0650, 0.0650 );
		aMeasurements->addRecord( 1.800, 0.7770, 0.0650, 0.0650 );
		aMeasurements->addRecord( 1.850, 0.7760, 0.0650, 0.0630 );
		aMeasurements->addRecord( 1.900, 0.7730, 0.0620, 0.0620 );
		aMeasurements->addRecord( 1.950, 0.7730, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.000, 0.7720, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.050, 0.7740, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.100, 0.7750, 0.0640, 0.0650 );
		aMeasurements->addRecord( 2.150, 0.7730, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.200, 0.7580, 0.0640, 0.0650 );
		aMeasurements->addRecord( 2.250, 0.7590, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.300, 0.7660, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.350, 0.7670, 0.0640, 0.0650 );
		aMeasurements->addRecord( 2.400, 0.7660, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.450, 0.7570, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.500, 0.7500, 0.0630, 0.0630 );

		return aMeasurements;
	}

	void SetUp() override {

		auto aSolarRadiation = getSolarRadiation();

		auto aMeasurements = getMeasurements();

		auto aSample = std::make_shared< CSpectralSample >( aMeasurements, aSolarRadiation );

		auto thickness = 5.715e-3; // [m]
		auto layerType = MaterialType::Monolithic;

		m_Sample = std::make_shared< CAngularSpectralSample >( aSample, thickness, layerType );

	}

public:
	std::shared_ptr< CAngularSpectralSample > getSample() const {
		return m_Sample;
	};

};

TEST_F( TestSampleNFRC_103_Angular, TestProperties0degrees ) {

	auto angle = 0.0;

	std::shared_ptr< CAngularSpectralSample > angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.77068014770698934, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.069984173508366929, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.070250681323265077, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.15933567878464375, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties10degrees ) {

	auto angle = 10.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.76980319121439578, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.069950630413725984, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.070217043956113862, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.16024617837187857, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties20degrees ) {

	auto angle = 20.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.76691124365416619, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.070135520990238370, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.070401987567688062, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.16295323535559544, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties30degrees ) {

	auto angle = 30.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.76103766815923068, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.071607090478364152, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.071874821559627849, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.16735524136240523, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties40degrees ) {

	auto angle = 40.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.74979572701044594, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.077031089090913732, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.077303147565901148, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.17317318389864053, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties50degrees ) {

	auto angle = 50.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.72747576073993681, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.092802977512975046, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.093084622790465060, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.17972126174708844, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties60degrees ) {

	auto angle = 60.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.68021254285214183, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.13439623099689185, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.13469298221439049, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.18539122615096648, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties70degrees ) {

	auto angle = 70.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.57483609737842067, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.23889023227286083, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.23919930021548919, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.18627367034871853, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties80degrees ) {

	auto angle = 80.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.34587632310986616, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.48399936961291273, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.48427878594468138, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0.17012430727722111, absorptance, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular, TestProperties90degrees ) {

	auto angle = 90.0;

	auto angularSample = getSample();

	// SOLAR RANGE
	auto lowLambda = 0.3;
	auto highLambda = 2.5;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 1, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 1, reflectanceBack, 1e-6 );

	auto absorptance = angularSample->getProperty( lowLambda, highLambda, Property::Abs, Side::Front, angle );
	EXPECT_NEAR( 0., absorptance, 1e-6 );

}
