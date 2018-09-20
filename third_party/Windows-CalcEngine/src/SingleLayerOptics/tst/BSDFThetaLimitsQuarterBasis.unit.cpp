#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;

class TestBSDFThetaLimtisQuarterBasis : public testing::Test {

private:
	std::shared_ptr< CThetaLimits > m_Thetas;

protected:
	virtual void SetUp() {
		std::vector< double > thetaAngles = { 0, 18, 36, 54, 76.5 };
		m_Thetas = std::make_shared< CThetaLimits >( thetaAngles );
	}

public:
	std::shared_ptr< CThetaLimits > GetLimits() {
		return m_Thetas;
	};

};

TEST_F( TestBSDFThetaLimtisQuarterBasis, TestQuarterBasis ) {
	SCOPED_TRACE( "Begin Test: Theta limits - quarter basis." );

	std::shared_ptr< CThetaLimits > aLimits = GetLimits();

	std::vector< double > results = *( aLimits->getThetaLimits() );

	std::vector< double > correctResults = { 0, 9, 27, 45, 63, 90 };
	EXPECT_EQ( results.size(), correctResults.size() );

	for ( size_t i = 0; i < results.size(); ++i ) {
		EXPECT_NEAR( results[i], correctResults[i], 1e-6 );
	}

}
