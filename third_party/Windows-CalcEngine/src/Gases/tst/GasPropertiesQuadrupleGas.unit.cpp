#include <memory>
#include <gtest/gtest.h>

#include "WCEGases.hpp"

using namespace Gases;

class TestGasPropertiesQuadrupleGas : public testing::Test {

protected:
	CGas m_Gas;

	virtual void SetUp() {
		// Create coefficients for Air
		CIntCoeff AirCon{ 2.8733e-03, 7.76e-05, 0.0 };
		CIntCoeff AirCp{ 1.002737e+03, 1.2324e-02, 0.0 };
		CIntCoeff AirVisc{ 3.7233e-06, 4.94e-08, 0.0 };

		CGasData const AirData{ "Air", 28.97, 1.4, AirCp, AirCon, AirVisc };

		// Create coefficients for Argon
		CIntCoeff ArgonCon{ 2.2848e-03, 5.1486e-05, 0.0 };
		CIntCoeff ArgonCp{ 5.21929e+02, 0.0, 0.0 };
		CIntCoeff ArgonVisc{ 3.3786e-06, 6.4514e-08, 0.0 };

		CGasData const ArgonData{ "Argon", 39.948, 1.67, ArgonCp, ArgonCon, ArgonVisc };

		// Create coefficients for Krypton
		CIntCoeff KryptonCon{ 9.443e-04, 2.8260e-5, 0.0 };
		CIntCoeff KryptonCp{ 2.4809e+02, 0.0, 0.0 };
		CIntCoeff KryptonVisc{ 2.213e-6, 7.777e-8, 0.0 };

		CGasData const KryptonData{ "Krypton", 83.8, 1.68, KryptonCp, KryptonCon, KryptonVisc };

		// Create coefficients for Xenon
		CIntCoeff XenonCon{ 4.538e-04, 1.723e-05, 0.0 };
		CIntCoeff XenonCp{ 1.5834e+02, 0.0, 0.0 };
		CIntCoeff XenonVisc{ 1.069e-6, 7.414e-8, 0.0 };

		CGasData const XenonData{ "Xenon", 131.3, 1.66, XenonCp, XenonCon, XenonVisc };

		CGasItem Air{ 0.1, AirData };
		CGasItem Argon{ 0.3, ArgonData };
		CGasItem Krypton{ 0.3, KryptonData };
		CGasItem Xenon{ 0.3, XenonData };

		m_Gas.addGasItem( Air );
		m_Gas.addGasItem( Argon );
		m_Gas.addGasItem( Krypton );
		m_Gas.addGasItem( Xenon );
	}

};

TEST_F( TestGasPropertiesQuadrupleGas, TestSimpleProperties ) {
	SCOPED_TRACE( "Begin Test: Gas Properties (quadruple gas) simple mix - Temperature = 300 [K], Pressure = 101325 [Pa]" );

	m_Gas.setTemperatureAndPressure( 300, 101325 );
	auto aProperties = m_Gas.getSimpleGasProperties();

	EXPECT_NEAR( 79.4114, aProperties->m_MolecularWeight, 0.0001 );
	EXPECT_NEAR( 1.24480400E-02, aProperties->m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 2.33306700E-05, aProperties->m_Viscosity, 1e-6 );
	EXPECT_NEAR( 379.15142, aProperties->m_SpecificHeat, 0.001 );
	EXPECT_NEAR( 3.225849103, aProperties->m_Density, 0.0001 );
	EXPECT_NEAR( 1.01775733E-05, aProperties->m_Alpha, 1e-6 );
	EXPECT_NEAR( 0.710622448, aProperties->m_PrandlNumber, 0.0001 );
}

TEST_F( TestGasPropertiesQuadrupleGas, TestSimplePropertiesRepeat ) {
	SCOPED_TRACE( "Begin Test: Gas Properties (quadruple gas) simple mix - Temperature = 300 [K], Pressure = 101325 [Pa] (Repeatability)" );

	m_Gas.setTemperatureAndPressure( 300, 101325 );
	auto aProperties = m_Gas.getSimpleGasProperties();

	EXPECT_NEAR( 79.4114, aProperties->m_MolecularWeight, 0.0001 );
	EXPECT_NEAR( 1.24480400E-02, aProperties->m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 2.33306700E-05, aProperties->m_Viscosity, 1e-6 );
	EXPECT_NEAR( 379.15142, aProperties->m_SpecificHeat, 0.001 );
	EXPECT_NEAR( 3.225849103, aProperties->m_Density, 0.0001 );
	EXPECT_NEAR( 1.01775733E-05, aProperties->m_Alpha, 1e-6 );
	EXPECT_NEAR( 0.710622448, aProperties->m_PrandlNumber, 0.0001 );
}

TEST_F( TestGasPropertiesQuadrupleGas, TestRealProperties ) {
	SCOPED_TRACE( "Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], Pressure = 101325 [Pa]" );

	m_Gas.setTemperatureAndPressure( 300, 101325 );
	auto aProperties = m_Gas.getGasProperties();

	EXPECT_NEAR( 79.4114, aProperties->m_MolecularWeight, 0.0001 );
	EXPECT_NEAR( 1.108977555E-02, aProperties->m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 2.412413749E-05, aProperties->m_Viscosity, 1e-6 );
	EXPECT_NEAR( 272.5637141, aProperties->m_SpecificHeat, 0.001 );
	EXPECT_NEAR( 3.225849103, aProperties->m_Density, 0.0001 );
	EXPECT_NEAR( 1.26127756E-05, aProperties->m_Alpha, 1e-6 );
	EXPECT_NEAR( 0.592921334, aProperties->m_PrandlNumber, 0.0001 );
}

TEST_F( TestGasPropertiesQuadrupleGas, TestRealPropertiesRepeat ) {
	SCOPED_TRACE( "Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], Pressure = 101325 [Pa] (Repeatability)" );

	m_Gas.setTemperatureAndPressure( 300, 101325 );
	auto aProperties = m_Gas.getGasProperties();

	EXPECT_NEAR( 79.4114, aProperties->m_MolecularWeight, 0.0001 );
	EXPECT_NEAR( 1.108977555E-02, aProperties->m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 2.412413749E-05, aProperties->m_Viscosity, 1e-6 );
	EXPECT_NEAR( 272.5637141, aProperties->m_SpecificHeat, 0.001 );
	EXPECT_NEAR( 3.225849103, aProperties->m_Density, 0.0001 );
	EXPECT_NEAR( 1.26127756E-05, aProperties->m_Alpha, 1e-6 );
	EXPECT_NEAR( 0.592921334, aProperties->m_PrandlNumber, 0.0001 );
}

TEST_F( TestGasPropertiesQuadrupleGas, TestRealPropertiesLowPressure ) {
	SCOPED_TRACE( "Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], Pressure = 90,000 [Pa]" );

	m_Gas.setTemperatureAndPressure( 300, 90000 );
	auto aProperties = m_Gas.getGasProperties();

	EXPECT_NEAR( 79.4114, aProperties->m_MolecularWeight, 0.0001 );
	EXPECT_NEAR( 1.108977555E-02, aProperties->m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 2.412413749E-05, aProperties->m_Viscosity, 1e-6 );
	EXPECT_NEAR( 272.5637141, aProperties->m_SpecificHeat, 0.001 );
	EXPECT_NEAR( 2.865298981, aProperties->m_Density, 0.0001 );
	EXPECT_NEAR( 1.41998832E-05, aProperties->m_Alpha, 1e-6 );
	EXPECT_NEAR( 0.592921334, aProperties->m_PrandlNumber, 0.0001 );
}

TEST_F( TestGasPropertiesQuadrupleGas, TestRealPropertiesLowPressureRepeat ) {
	SCOPED_TRACE( "Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], Pressure = 90,000 [Pa] (Repeatability)" );

	m_Gas.setTemperatureAndPressure( 300, 90000 );
	auto aProperties = m_Gas.getGasProperties();

	EXPECT_NEAR( 79.4114, aProperties->m_MolecularWeight, 0.0001 );
	EXPECT_NEAR( 1.108977555E-02, aProperties->m_ThermalConductivity, 1e-6 );
	EXPECT_NEAR( 2.412413749E-05, aProperties->m_Viscosity, 1e-6 );
	EXPECT_NEAR( 272.5637141, aProperties->m_SpecificHeat, 0.001 );
	EXPECT_NEAR( 2.865298981, aProperties->m_Density, 0.0001 );
	EXPECT_NEAR( 1.41998832E-05, aProperties->m_Alpha, 1e-6 );
	EXPECT_NEAR( 0.592921334, aProperties->m_PrandlNumber, 0.0001 );
}

TEST_F(TestGasPropertiesQuadrupleGas, TotalPercents) {
	SCOPED_TRACE( "Begin Test: Gas Properties (quadruple gas) - Total percents." );

	double percents = m_Gas.totalPercent();

	ASSERT_EQ( 1.0, percents );
}
