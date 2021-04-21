#include <memory>
#include <gtest/gtest.h>

#include "WCEGases.hpp"


using namespace Gases;

class TestGasPropertiesVacuum : public testing::Test {

private:
	CGas Gas; // Default gas is 100% air

public:
	CGas & GetGas() {
		return Gas;
	};

};

TEST_F( TestGasPropertiesVacuum, TestVacuumProperties1 ) {
	SCOPED_TRACE( "Begin Test: Gas Vacuum Properties (Air) - Temperature = 273.15 [K], Pressure = 0.1333 [Pa]" );

	auto aGas = GetGas();

	aGas.setTemperatureAndPressure( 273.15, 0.1333 );
	auto aProperties = aGas.getGasProperties();

	EXPECT_NEAR( 28.97, aProperties.m_MolecularWeight, 1e-6 );
	EXPECT_NEAR( 0.106769062, aProperties.m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_Viscosity, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_SpecificHeat, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_Density, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_Alpha, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_PrandlNumber, 1e-6 );

}

TEST_F( TestGasPropertiesVacuum, TestVacuumProperties2 ) {
	SCOPED_TRACE( "Begin Test: Gas Vacuum Properties (Air) - Temperature = 293.15 [K], Pressure = 0.1333 [Pa]" );

	auto aGas = GetGas();

	aGas.setTemperatureAndPressure( 293.15, 0.1333 );
	auto aProperties = aGas.getGasProperties();

	EXPECT_NEAR( 28.97, aProperties.m_MolecularWeight, 1e-6 );
	EXPECT_NEAR( 0.1030625965, aProperties.m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_Viscosity, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_SpecificHeat, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_Density, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_Alpha, 1e-6 );
	EXPECT_NEAR( 0, aProperties.m_PrandlNumber, 1e-6 );

}
