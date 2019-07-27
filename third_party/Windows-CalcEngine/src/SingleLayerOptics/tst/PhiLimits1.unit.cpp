#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;

class TestPhiLimits1 : public testing::Test {

private:
	std::shared_ptr< CPhiLimits > m_PhiLimits;

protected:
	virtual void SetUp() {
		m_PhiLimits = std::make_shared< CPhiLimits >( 8 );
	}

public:
	std::shared_ptr< CPhiLimits > GetLimits() {
		return m_PhiLimits;
	};

};

TEST_F( TestPhiLimits1, TestBSDFRingCreation ) {
	SCOPED_TRACE( "Begin Test: BSDF Phi limits creation." );

	std::shared_ptr< CPhiLimits > aLimits = GetLimits();

	std::vector< double > results = *( aLimits->getPhiLimits() );

	std::vector< double > correctResults = { -22.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5 };
	EXPECT_EQ( results.size(), correctResults.size() );

	for ( size_t i = 0; i < results.size(); ++i ) {
		EXPECT_NEAR( results[i], correctResults[i], 1e-6 );
	}

}
