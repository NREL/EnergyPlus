#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;

class TestPhisAngles2 : public testing::Test {

private:
	std::shared_ptr< CBSDFPhiAngles > m_BasisRing;

protected:
	virtual void SetUp() {
		m_BasisRing = std::make_shared< CBSDFPhiAngles >( 12 );
	}

public:
	std::shared_ptr< CBSDFPhiAngles > GetRing() {
		return m_BasisRing;
	};

};

TEST_F( TestPhisAngles2, TestBSDFRingCreation ) {
	SCOPED_TRACE( "Begin Test: Phi angles creation." );

	std::shared_ptr< CBSDFPhiAngles > aRing = GetRing();

	std::vector< double > results = *( aRing->phiAngles() );

	std::vector< double > correctResults = { 0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330 };
	EXPECT_EQ( results.size(), correctResults.size() );

	for ( size_t i = 0; i < results.size(); ++i ) {
		EXPECT_NEAR( results[i], correctResults[i], 1e-6 );
	}

}
