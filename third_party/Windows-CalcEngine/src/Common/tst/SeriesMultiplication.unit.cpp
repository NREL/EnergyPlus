#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

class TestSeriesMultiplication : public testing::Test {

private:
	std::shared_ptr< CSeries > m_Series;

protected:
	void SetUp() override {
		m_Series = make_shared< CSeries >();

		CSeries astmSolarRadiation = CSeries();

		// part of ASTM E891-87 Table 1
		astmSolarRadiation.addProperty( 0.50, 1026.7 );
		astmSolarRadiation.addProperty( 0.51, 1066.7 );
		astmSolarRadiation.addProperty( 0.52, 1011.5 );
		astmSolarRadiation.addProperty( 0.53, 1084.9 );
		astmSolarRadiation.addProperty( 0.54, 1082.4 );
		astmSolarRadiation.addProperty( 0.55, 1102.2 );
		astmSolarRadiation.addProperty( 0.57, 1087.4 );
		astmSolarRadiation.addProperty( 0.59, 1024.3 );
		astmSolarRadiation.addProperty( 0.61, 1088.8 );
		astmSolarRadiation.addProperty( 0.63, 1062.1 );
		astmSolarRadiation.addProperty( 0.65, 1061.7 );
		astmSolarRadiation.addProperty( 0.67, 1046.2 );
		astmSolarRadiation.addProperty( 0.69, 859.2 );
		astmSolarRadiation.addProperty( 0.71, 1002.4 );

		CSeries layerTransmittances = CSeries();

		layerTransmittances.addProperty( 0.500, 0.6928 );
		layerTransmittances.addProperty( 0.510, 0.7004 );
		layerTransmittances.addProperty( 0.520, 0.7067 );
		layerTransmittances.addProperty( 0.530, 0.7127 );
		layerTransmittances.addProperty( 0.540, 0.7179 );
		layerTransmittances.addProperty( 0.550, 0.7224 );
		layerTransmittances.addProperty( 0.570, 0.7267 );
		layerTransmittances.addProperty( 0.590, 0.7249 );
		layerTransmittances.addProperty( 0.610, 0.7187 );
		layerTransmittances.addProperty( 0.630, 0.7078 );
		layerTransmittances.addProperty( 0.650, 0.6916 );
		layerTransmittances.addProperty( 0.670, 0.6723 );
		layerTransmittances.addProperty( 0.690, 0.6492 );
		layerTransmittances.addProperty( 0.710, 0.6231 );

		m_Series = layerTransmittances.mMult( astmSolarRadiation );


	}

public:
	std::shared_ptr< CSeries > getProperty() {
		return m_Series;
	};

};

TEST_F( TestSeriesMultiplication, TestMultiplication ) {
	SCOPED_TRACE( "Begin Test: Test multiplication over the range of data." );

	CSeries& aSpectralProperties = *getProperty();

	vector< double > correctResults;
	correctResults.push_back( 711.29776 );
	correctResults.push_back( 747.11668 );
	correctResults.push_back( 714.82705 );
	correctResults.push_back( 773.20823 );
	correctResults.push_back( 777.05496 );
	correctResults.push_back( 796.22928 );
	correctResults.push_back( 790.21358 );
	correctResults.push_back( 742.51507 );
	correctResults.push_back( 782.52056 );
	correctResults.push_back( 751.75438 );
	correctResults.push_back( 734.27172 );
	correctResults.push_back( 703.36026 );
	correctResults.push_back( 557.79264 );
	correctResults.push_back( 624.59544 );

	EXPECT_EQ( aSpectralProperties.size(), correctResults.size() );

	for ( size_t i = 0; i < aSpectralProperties.size(); ++i ) {
		EXPECT_NEAR( correctResults[ i ], aSpectralProperties[ i ].value(), 1e-6 );
	}

}
