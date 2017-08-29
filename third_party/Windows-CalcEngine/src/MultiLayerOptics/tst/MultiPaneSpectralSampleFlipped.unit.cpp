#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace SpectralAveraging;
using namespace FenestrationCommon;
using namespace MultiLayerOptics;

class TestMultiLayerOpticsSampleFlipped : public testing::Test {

private:
	std::shared_ptr< CMultiPaneSpectralSample > m_MultiLayerOptics;

protected:
	virtual void SetUp() {
		std::shared_ptr< CSeries > solarRadiation = make_shared< CSeries >();
		solarRadiation->addProperty( 0.50, 1026.70 );
		solarRadiation->addProperty( 0.51, 1066.70 );
		solarRadiation->addProperty( 0.52, 1011.50 );
		solarRadiation->addProperty( 0.53, 1084.90 );
		solarRadiation->addProperty( 0.54, 1082.40 );
		solarRadiation->addProperty( 0.55, 1102.20 );
		solarRadiation->addProperty( 0.57, 1087.40 );
		solarRadiation->addProperty( 0.59, 1024.30 );
		solarRadiation->addProperty( 0.61, 1088.80 );
		solarRadiation->addProperty( 0.63, 1062.10 );
		solarRadiation->addProperty( 0.65, 1061.70 );
		solarRadiation->addProperty( 0.67, 1046.20 );
		solarRadiation->addProperty( 0.69, 859.20 );
		solarRadiation->addProperty( 0.71, 1002.40 );

		std::shared_ptr< CSpectralSampleData > sampleMeasurements1 = make_shared< CSpectralSampleData >();
		sampleMeasurements1->addRecord( 0.500, 0.6928, 0.2449, 0.1892 );
		sampleMeasurements1->addRecord( 0.505, 0.6968, 0.2406, 0.1840 );
		sampleMeasurements1->addRecord( 0.510, 0.7004, 0.2365, 0.1792 );
		sampleMeasurements1->addRecord( 0.515, 0.7036, 0.2324, 0.1744 );
		sampleMeasurements1->addRecord( 0.520, 0.7067, 0.2284, 0.1697 );
		sampleMeasurements1->addRecord( 0.525, 0.7099, 0.2244, 0.1650 );
		sampleMeasurements1->addRecord( 0.530, 0.7127, 0.2206, 0.1606 );
		sampleMeasurements1->addRecord( 0.535, 0.7156, 0.2167, 0.1563 );
		sampleMeasurements1->addRecord( 0.540, 0.7179, 0.2130, 0.1521 );
		sampleMeasurements1->addRecord( 0.545, 0.7200, 0.2094, 0.1481 );
		sampleMeasurements1->addRecord( 0.550, 0.7224, 0.2061, 0.1444 );
		sampleMeasurements1->addRecord( 0.555, 0.7244, 0.2029, 0.1409 );
		sampleMeasurements1->addRecord( 0.560, 0.7259, 0.1997, 0.1375 );
		sampleMeasurements1->addRecord( 0.565, 0.7265, 0.1967, 0.1344 );
		sampleMeasurements1->addRecord( 0.570, 0.7267, 0.1940, 0.1317 );
		sampleMeasurements1->addRecord( 0.575, 0.7268, 0.1914, 0.1292 );
		sampleMeasurements1->addRecord( 0.580, 0.7267, 0.1889, 0.1270 );
		sampleMeasurements1->addRecord( 0.585, 0.7257, 0.1868, 0.1252 );
		sampleMeasurements1->addRecord( 0.590, 0.7249, 0.1849, 0.1237 );
		sampleMeasurements1->addRecord( 0.595, 0.7237, 0.1835, 0.1224 );
		sampleMeasurements1->addRecord( 0.600, 0.7228, 0.1825, 0.1217 );
		sampleMeasurements1->addRecord( 0.605, 0.7211, 0.1815, 0.1211 );
		sampleMeasurements1->addRecord( 0.610, 0.7187, 0.1806, 0.1207 );
		sampleMeasurements1->addRecord( 0.615, 0.7164, 0.1802, 0.1209 );
		sampleMeasurements1->addRecord( 0.620, 0.7138, 0.1801, 0.1214 );
		sampleMeasurements1->addRecord( 0.625, 0.7110, 0.1802, 0.1223 );
		sampleMeasurements1->addRecord( 0.630, 0.7078, 0.1807, 0.1236 );
		sampleMeasurements1->addRecord( 0.635, 0.7037, 0.1813, 0.1251 );
		sampleMeasurements1->addRecord( 0.640, 0.6999, 0.1823, 0.1269 );
		sampleMeasurements1->addRecord( 0.645, 0.6958, 0.1837, 0.1292 );
		sampleMeasurements1->addRecord( 0.650, 0.6916, 0.1849, 0.1317 );
		sampleMeasurements1->addRecord( 0.655, 0.6870, 0.1865, 0.1345 );
		sampleMeasurements1->addRecord( 0.660, 0.6823, 0.1885, 0.1377 );
		sampleMeasurements1->addRecord( 0.665, 0.6773, 0.1906, 0.1411 );
		sampleMeasurements1->addRecord( 0.670, 0.6723, 0.1931, 0.1451 );
		sampleMeasurements1->addRecord( 0.675, 0.6670, 0.1957, 0.1491 );
		sampleMeasurements1->addRecord( 0.680, 0.6614, 0.1985, 0.1533 );
		sampleMeasurements1->addRecord( 0.685, 0.6551, 0.2015, 0.1580 );
		sampleMeasurements1->addRecord( 0.690, 0.6492, 0.2047, 0.1630 );
		sampleMeasurements1->addRecord( 0.695, 0.6432, 0.2080, 0.1682 );
		sampleMeasurements1->addRecord( 0.700, 0.6366, 0.2115, 0.1733 );

		std::shared_ptr< CSpectralSampleData > sampleMeasurements2 = make_shared< CSpectralSampleData >();
		sampleMeasurements2->addRecord( 0.500, 0.8940, 0.0820, 0.0820 );
		sampleMeasurements2->addRecord( 0.510, 0.8950, 0.0820, 0.0820 );
		sampleMeasurements2->addRecord( 0.520, 0.8950, 0.0820, 0.0820 );
		sampleMeasurements2->addRecord( 0.530, 0.8940, 0.0820, 0.0820 );
		sampleMeasurements2->addRecord( 0.540, 0.8930, 0.0810, 0.0810 );
		sampleMeasurements2->addRecord( 0.550, 0.8910, 0.0810, 0.0810 );
		sampleMeasurements2->addRecord( 0.560, 0.8880, 0.0810, 0.0810 );
		sampleMeasurements2->addRecord( 0.570, 0.8840, 0.0800, 0.0800 );
		sampleMeasurements2->addRecord( 0.580, 0.8810, 0.0800, 0.0800 );
		sampleMeasurements2->addRecord( 0.590, 0.8760, 0.0790, 0.0790 );
		sampleMeasurements2->addRecord( 0.600, 0.8710, 0.0790, 0.0790 );
		sampleMeasurements2->addRecord( 0.610, 0.8650, 0.0780, 0.0780 );
		sampleMeasurements2->addRecord( 0.620, 0.8590, 0.0770, 0.0770 );
		sampleMeasurements2->addRecord( 0.630, 0.8530, 0.0770, 0.0770 );
		sampleMeasurements2->addRecord( 0.640, 0.8470, 0.0760, 0.0760 );
		sampleMeasurements2->addRecord( 0.650, 0.8400, 0.0750, 0.0750 );
		sampleMeasurements2->addRecord( 0.660, 0.8330, 0.0750, 0.0750 );
		sampleMeasurements2->addRecord( 0.670, 0.8260, 0.0740, 0.0740 );
		sampleMeasurements2->addRecord( 0.680, 0.8180, 0.0730, 0.0730 );
		sampleMeasurements2->addRecord( 0.690, 0.8100, 0.0730, 0.0730 );
		sampleMeasurements2->addRecord( 0.700, 0.8020, 0.0720, 0.0720 );

		sampleMeasurements1->Filpped( true );

		std::shared_ptr< CMultiPaneSampleData > multiSample = make_shared< CMultiPaneSampleData >();
		multiSample->addSample( sampleMeasurements1 );
		multiSample->addSample( sampleMeasurements2 );

		m_MultiLayerOptics = make_shared< CMultiPaneSpectralSample >( multiSample, solarRadiation );
	}

public:
	std::shared_ptr< CMultiPaneSpectralSample > getMultiLayerOptics() {
		return m_MultiLayerOptics;
	};

};

TEST_F( TestMultiLayerOpticsSampleFlipped, TestDoublePaneProperties ) {
	SCOPED_TRACE( "Begin Test: Test double pane solar absorptances." );

	double lowLambda = 0.5;
	double highLambda = 0.7;

	auto MultiLayerOptics = getMultiLayerOptics();

	double transmittance = MultiLayerOptics->getProperty( lowLambda, highLambda, Property::T, Side::Front );

	EXPECT_NEAR( 0.618340, transmittance, 1e-6 );

	double frontReflectance = MultiLayerOptics->getProperty( lowLambda, highLambda, Property::R, Side::Front );

	EXPECT_NEAR( 0.181564, frontReflectance, 1e-6 );

	double backReflectance = MultiLayerOptics->getProperty( lowLambda, highLambda, Property::R, Side::Back );

	EXPECT_NEAR( 0.229538, backReflectance, 1e-6 );

	double totalAbsorptance = MultiLayerOptics->getProperty( lowLambda, highLambda, Property::Abs, Side::Front );

	EXPECT_NEAR( 0.200096, totalAbsorptance, 1e-6 );

}

TEST_F( TestMultiLayerOpticsSampleFlipped, TestDoublePaneLayerAbsorptances ) {
	SCOPED_TRACE( "Begin Test: Double pane layer absroptances." );

	double lowLambda = 0.5;
	double highLambda = 0.7;

	auto MultiLayerOptics = getMultiLayerOptics();

	double abs1 = MultiLayerOptics->getLayerAbsorptance( lowLambda, highLambda, 1 );
	EXPECT_NEAR( 0.159710, abs1, 1e-6 );

	double abs2 = MultiLayerOptics->getLayerAbsorptance( lowLambda, highLambda, 2 );
	EXPECT_NEAR( 0.040386, abs2, 1e-6 );

}
