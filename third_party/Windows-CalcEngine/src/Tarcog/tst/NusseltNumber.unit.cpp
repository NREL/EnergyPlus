#include <memory>
#include <stdexcept>
#include <iostream>

#include <gtest/gtest.h>

#include "WCETarcog.hpp"

using namespace Tarcog;
using namespace std;

class TestNusseltNumber : public testing::Test {

private:
	std::shared_ptr< CNusseltNumber > m_NusseltNumber;

protected:
	void SetUp() override {
		m_NusseltNumber = make_shared< CNusseltNumber >();
	}

public:
	std::shared_ptr< CNusseltNumber > GetNusselt() const {
		return m_NusseltNumber;
	};

};

TEST_F( TestNusseltNumber, NusseltNumberDifferentAngles_Test1 ) {
	SCOPED_TRACE( "Begin Test: Nusselt number (Test 1) - different angles" );

	auto aNusselt = GetNusselt();
	ASSERT_TRUE( aNusselt != nullptr );

	auto tTilt = 30.0;
	auto tRa = 3638.21667064528;
	auto tAsp = 83.3333333333333;
	auto nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.40474349200254, nusseltNumber, 1e-6 );

	tTilt = 60;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.08005742342789, nusseltNumber, 1e-6 );

	tTilt = 73;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.05703042079892, nusseltNumber, 1e-6 );

	tTilt = 90;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.02691818659179, nusseltNumber, 1e-6 );

	tTilt = 134;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.01936332296842, nusseltNumber, 1e-6 );
}

TEST_F( TestNusseltNumber, NusseltNumberDifferentAngles_Test2 ) {
	SCOPED_TRACE( "Begin Test: Nusselt number (Test 2) - different angles" );

	auto aNusselt = GetNusselt();
	ASSERT_TRUE( aNusselt != nullptr );

	auto tTilt = 30.0;
	auto tRa = 140.779077041012;
	auto tAsp = 200.0;
	auto nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.00000000000000, nusseltNumber, 1e-6 );

	tTilt = 60;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.00002777439094, nusseltNumber, 1e-6 );

	tTilt = 73;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.00002235511865, nusseltNumber, 1e-6 );

	tTilt = 90;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.00001526837795, nusseltNumber, 1e-6 );

	tTilt = 134;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 1.00001098315195, nusseltNumber, 1e-6 );

}

TEST_F( TestNusseltNumber, NusseltNumberDifferentAngles_Test3 ) {
	SCOPED_TRACE( "Begin Test: Nusselt number (Test 3) - different angles" );

	auto aNusselt = GetNusselt();
	ASSERT_TRUE( aNusselt != nullptr );

	auto tTilt = 30.0;
	auto tRa = 4633340.8866717;
	auto tAsp = 10.0;
	auto nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 10.2680981545288, nusseltNumber, 1e-6 );

	tTilt = 60;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 11.5975502261096, nusseltNumber, 1e-6 );

	tTilt = 73;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 11.4398529673101, nusseltNumber, 1e-6 );

	tTilt = 90;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 11.2336334750340, nusseltNumber, 1e-6 );

	tTilt = 134;
	nusseltNumber = aNusselt->calculate( tTilt, tRa, tAsp );
	EXPECT_NEAR( 8.36145986066987, nusseltNumber, 1e-6 );

}
