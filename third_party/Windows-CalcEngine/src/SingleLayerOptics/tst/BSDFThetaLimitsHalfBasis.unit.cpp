#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;

class TestBSDFThetaLimtisHalfBasis : public testing::Test {

private:
	std::shared_ptr< CThetaLimits > m_Thetas;

protected:
	virtual void SetUp() {
		std::vector< double > thetaAngles = { 0, 13, 26, 39, 52, 65, 80.75 };
		m_Thetas = std::make_shared< CThetaLimits >( thetaAngles );
	}

public:
	std::shared_ptr< CThetaLimits > GetLimits() {
		return m_Thetas;
	};

};

TEST_F( TestBSDFThetaLimtisHalfBasis, TestHalfBasis ) {
	SCOPED_TRACE( "Begin Test: Theta limits - half basis." );

	std::shared_ptr< CThetaLimits > aLimits = GetLimits();

	std::vector< double > results = *( aLimits->getThetaLimits() );

	std::vector< double > correctResults = { 0, 6.5, 19.5, 32.5, 45.5, 58.5, 71.5, 90 };
	EXPECT_EQ( results.size(), correctResults.size() );

	for ( size_t i = 0; i < results.size(); ++i ) {
		EXPECT_NEAR( results[i], correctResults[i], 1e-6 );
	}

}
