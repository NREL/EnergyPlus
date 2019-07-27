#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;
using namespace SpectralAveraging;

class TestNIRRatio : public testing::Test {

private:
	std::shared_ptr< CNIRRatio > m_NIRRatio;

protected:
	std::shared_ptr< CSeries > getSolarRadiation() const {
		auto solarRadiation = std::make_shared< CSeries >();

		// Full ASTM E891-87 Table 1
		solarRadiation->addProperty( 0.3000, 0.0 );
		solarRadiation->addProperty( 0.3050, 3.4 );
		solarRadiation->addProperty( 0.3100, 15.6 );
		solarRadiation->addProperty( 0.3150, 41.1 );
		solarRadiation->addProperty( 0.3200, 71.2 );
		solarRadiation->addProperty( 0.3250, 100.2 );
		solarRadiation->addProperty( 0.3300, 152.4 );
		solarRadiation->addProperty( 0.3350, 155.6 );
		solarRadiation->addProperty( 0.3400, 179.4 );
		solarRadiation->addProperty( 0.3450, 186.7 );
		solarRadiation->addProperty( 0.3500, 212.0 );
		solarRadiation->addProperty( 0.3600, 240.5 );
		solarRadiation->addProperty( 0.3700, 324.0 );
		solarRadiation->addProperty( 0.3800, 362.4 );
		solarRadiation->addProperty( 0.3900, 381.7 );
		solarRadiation->addProperty( 0.4000, 556.0 );
		solarRadiation->addProperty( 0.4100, 656.3 );
		solarRadiation->addProperty( 0.4200, 690.8 );
		solarRadiation->addProperty( 0.4300, 641.9 );
		solarRadiation->addProperty( 0.4400, 798.5 );
		solarRadiation->addProperty( 0.4500, 956.6 );
		solarRadiation->addProperty( 0.4600, 990.0 );
		solarRadiation->addProperty( 0.4700, 998.0 );
		solarRadiation->addProperty( 0.4800, 1046.1 );
		solarRadiation->addProperty( 0.4900, 1005.1 );
		solarRadiation->addProperty( 0.5000, 1026.7 );
		solarRadiation->addProperty( 0.5100, 1066.7 );
		solarRadiation->addProperty( 0.5200, 1011.5 );
		solarRadiation->addProperty( 0.5300, 1084.9 );
		solarRadiation->addProperty( 0.5400, 1082.4 );
		solarRadiation->addProperty( 0.5500, 1102.2 );
		solarRadiation->addProperty( 0.5700, 1087.4 );
		solarRadiation->addProperty( 0.5900, 1024.3 );
		solarRadiation->addProperty( 0.6100, 1088.8 );
		solarRadiation->addProperty( 0.6300, 1062.1 );
		solarRadiation->addProperty( 0.6500, 1061.7 );
		solarRadiation->addProperty( 0.6700, 1046.2 );
		solarRadiation->addProperty( 0.6900, 859.2 );
		solarRadiation->addProperty( 0.7100, 1002.4 );
		solarRadiation->addProperty( 0.7180, 816.9 );
		solarRadiation->addProperty( 0.7244, 842.8 );
		solarRadiation->addProperty( 0.7400, 971.0 );
		solarRadiation->addProperty( 0.7525, 956.3 );
		solarRadiation->addProperty( 0.7575, 942.2 );
		solarRadiation->addProperty( 0.7625, 524.8 );
		solarRadiation->addProperty( 0.7675, 830.7 );
		solarRadiation->addProperty( 0.7800, 908.9 );
		solarRadiation->addProperty( 0.8000, 873.4 );
		solarRadiation->addProperty( 0.8160, 712.0 );
		solarRadiation->addProperty( 0.8237, 660.2 );
		solarRadiation->addProperty( 0.8315, 765.5 );
		solarRadiation->addProperty( 0.8400, 799.8 );
		solarRadiation->addProperty( 0.8600, 815.2 );
		solarRadiation->addProperty( 0.8800, 778.3 );
		solarRadiation->addProperty( 0.9050, 630.4 );
		solarRadiation->addProperty( 0.9150, 565.2 );
		solarRadiation->addProperty( 0.9250, 586.4 );
		solarRadiation->addProperty( 0.9300, 348.1 );
		solarRadiation->addProperty( 0.9370, 224.2 );
		solarRadiation->addProperty( 0.9480, 271.4 );
		solarRadiation->addProperty( 0.9650, 451.2 );
		solarRadiation->addProperty( 0.9800, 549.7 );
		solarRadiation->addProperty( 0.9935, 630.1 );
		solarRadiation->addProperty( 1.0400, 582.9 );
		solarRadiation->addProperty( 1.0700, 539.7 );
		solarRadiation->addProperty( 1.1000, 366.2 );
		solarRadiation->addProperty( 1.1200, 98.1 );
		solarRadiation->addProperty( 1.1300, 169.5 );
		solarRadiation->addProperty( 1.1370, 118.7 );
		solarRadiation->addProperty( 1.1610, 301.9 );
		solarRadiation->addProperty( 1.1800, 406.8 );
		solarRadiation->addProperty( 1.2000, 375.2 );
		solarRadiation->addProperty( 1.2350, 423.6 );
		solarRadiation->addProperty( 1.2900, 365.7 );
		solarRadiation->addProperty( 1.3200, 223.4 );
		solarRadiation->addProperty( 1.3500, 30.1 );
		solarRadiation->addProperty( 1.3950, 1.4 );
		solarRadiation->addProperty( 1.4425, 51.6 );
		solarRadiation->addProperty( 1.4625, 97.0 );
		solarRadiation->addProperty( 1.4770, 97.3 );
		solarRadiation->addProperty( 1.4970, 167.1 );
		solarRadiation->addProperty( 1.5200, 239.3 );
		solarRadiation->addProperty( 1.5390, 248.8 );
		solarRadiation->addProperty( 1.5580, 249.3 );
		solarRadiation->addProperty( 1.5780, 222.3 );
		solarRadiation->addProperty( 1.5920, 227.3 );
		solarRadiation->addProperty( 1.6100, 210.5 );
		solarRadiation->addProperty( 1.6300, 224.7 );
		solarRadiation->addProperty( 1.6460, 215.9 );
		solarRadiation->addProperty( 1.6780, 202.8 );
		solarRadiation->addProperty( 1.7400, 158.2 );
		solarRadiation->addProperty( 1.8000, 28.6 );
		solarRadiation->addProperty( 1.8600, 1.8 );
		solarRadiation->addProperty( 1.9200, 1.1 );
		solarRadiation->addProperty( 1.9600, 19.7 );
		solarRadiation->addProperty( 1.9850, 84.9 );
		solarRadiation->addProperty( 2.0050, 25.0 );
		solarRadiation->addProperty( 2.0350, 92.5 );
		solarRadiation->addProperty( 2.0650, 56.3 );
		solarRadiation->addProperty( 2.1000, 82.7 );
		solarRadiation->addProperty( 2.1480, 76.2 );
		solarRadiation->addProperty( 2.1980, 66.4 );
		solarRadiation->addProperty( 2.2700, 65.0 );
		solarRadiation->addProperty( 2.3600, 57.6 );
		solarRadiation->addProperty( 2.4500, 19.8 );
		solarRadiation->addProperty( 2.4940, 17.0 );
		solarRadiation->addProperty( 2.5370, 3.0 );
		solarRadiation->addProperty( 2.9410, 4.0 );
		solarRadiation->addProperty( 2.9730, 7.0 );
		solarRadiation->addProperty( 3.0050, 6.0 );
		solarRadiation->addProperty( 3.0560, 3.0 );
		solarRadiation->addProperty( 3.1320, 5.0 );
		solarRadiation->addProperty( 3.1560, 18.0 );
		solarRadiation->addProperty( 3.2040, 1.2 );
		solarRadiation->addProperty( 3.2450, 3.0 );
		solarRadiation->addProperty( 3.3170, 12.0 );
		solarRadiation->addProperty( 3.3440, 3.0 );
		solarRadiation->addProperty( 3.4500, 12.2 );
		solarRadiation->addProperty( 3.5730, 11.0 );
		solarRadiation->addProperty( 3.7650, 9.0 );
		solarRadiation->addProperty( 4.0450, 6.9 );

		return solarRadiation;
	}

	void SetUp() override {
		auto solarRadiation = getSolarRadiation();

		auto lowLambda = 0.38;
		auto highLambda = 0.78;

		m_NIRRatio = std::make_shared< CNIRRatio >( solarRadiation, lowLambda, highLambda );

	}

public:
	std::shared_ptr< CNIRRatio > getNIRRatio() const {
		return m_NIRRatio;
	};

};

TEST_F( TestNIRRatio, TestRatio ) {
	SCOPED_TRACE( "Begin Test: Test calculation of ratio." );

	auto aNIRRatio = *getNIRRatio();

	auto ratio = aNIRRatio.ratio();

	EXPECT_NEAR( 0.494586, ratio, 1e-6 );

}
