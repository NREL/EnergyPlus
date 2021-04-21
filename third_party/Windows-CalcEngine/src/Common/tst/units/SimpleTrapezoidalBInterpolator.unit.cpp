#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"


using namespace FenestrationCommon;

class TestSimpleTrapezoidalAIntegration : public testing::Test {

private:
	std::shared_ptr< IIntegratorStrategy > m_Integrator;

protected:
	void SetUp() override {
		CIntegratorFactory aFactory = CIntegratorFactory();
		m_Integrator = aFactory.getIntegrator( IntegrationType::TrapezoidalA );
	}

public:
	IIntegratorStrategy* getIntegrator() const {
		return m_Integrator.get();
	};

};

TEST_F( TestSimpleTrapezoidalAIntegration, TestTrapezoidalA ) {
	SCOPED_TRACE( "Begin Test: Test trapezoidal A integrator" );

	auto aIntegrator = getIntegrator();

	std::vector< std::unique_ptr< ISeriesPoint > > input;
	input.push_back( wce::make_unique< CSeriesPoint >( 10, 20 ) );
	input.push_back( wce::make_unique< CSeriesPoint >( 15, 30 ) );
	input.push_back( wce::make_unique< CSeriesPoint >( 20, 40 ) );

	const auto series = *aIntegrator->integrate( input );

	CSeries correctValues { { 10, 175 }, { 15, 275 } };

	EXPECT_EQ( correctValues.size(), series.size() );

	for( auto i = 0u ; i < correctValues.size() ; ++i ) {
		EXPECT_NEAR( correctValues[ i ].x(), series[ i ].x(), 1e-6 );
		EXPECT_NEAR( correctValues[ i ].value(), series[ i ].value(), 1e-6 );
	}

}
