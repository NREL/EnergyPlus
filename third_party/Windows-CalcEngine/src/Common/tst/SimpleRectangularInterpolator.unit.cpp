#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"


using namespace FenestrationCommon;

class TestSimpleRectangularIntegration : public testing::Test {

private:
	std::shared_ptr< IIntegratorStrategy > m_Integrator;

protected:
	void SetUp() override {
		CIntegratorFactory aFactory = CIntegratorFactory();
		m_Integrator = aFactory.getIntegrator( IntegrationType::Rectangular );
	}

public:
	IIntegratorStrategy* getIntegrator() const {
		return m_Integrator.get();
	};

};

TEST_F( TestSimpleRectangularIntegration, TestRectangular ) {
	SCOPED_TRACE( "Begin Test: Test rectangular integrator" );

	auto aIntegrator = getIntegrator();

	double value = aIntegrator->integrate( 1, 2, 10, 11 );

	EXPECT_NEAR( 10.00, value, 1e-6 );

}
