#include <memory>
#include <string>
#include <gtest/gtest.h>

#include "WCEGases.hpp"

using namespace Gases;

// Example that uses built in gas properties

class TestGasPropertiesQuadrupleGas : public testing::Test
{
protected:
    CGas m_Gas;

    virtual void SetUp()
    {
        m_Gas.addGasItem(0.1, GasDef::Air);
        m_Gas.addGasItem(0.3, GasDef::Argon);
        m_Gas.addGasItem(0.3, GasDef::Krypton);
        m_Gas.addGasItem(0.3, GasDef::Xenon);
    }
};

TEST_F(TestGasPropertiesQuadrupleGas, TestSimpleProperties)
{
    SCOPED_TRACE("Begin Test: Gas Properties (quadruple gas) simple mix - Temperature = 300 [K], "
                 "Pressure = 101325 [Pa]");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getSimpleGasProperties();

    EXPECT_NEAR(79.4114, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.24480400E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.33306700E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(379.15142, aProperties.m_SpecificHeat, 0.001);
    EXPECT_NEAR(3.225849103, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(1.01775733E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.710622448, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesQuadrupleGas, TestSimplePropertiesRepeat)
{
    SCOPED_TRACE("Begin Test: Gas Properties (quadruple gas) simple mix - Temperature = 300 [K], "
                 "Pressure = 101325 [Pa] (Repeatability)");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getSimpleGasProperties();

    EXPECT_NEAR(79.4114, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.24480400E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.33306700E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(379.15142, aProperties.m_SpecificHeat, 0.001);
    EXPECT_NEAR(3.225849103, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(1.01775733E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.710622448, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesQuadrupleGas, TestRealProperties)
{
    SCOPED_TRACE("Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], "
                 "Pressure = 101325 [Pa]");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(79.4114, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.108977555E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.412413749E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(272.5637141, aProperties.m_SpecificHeat, 0.001);
    EXPECT_NEAR(3.225849103, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(1.26127756E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.592921334, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesQuadrupleGas, TestRealPropertiesRepeat)
{
    SCOPED_TRACE("Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], "
                 "Pressure = 101325 [Pa] (Repeatability)");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(79.4114, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.108977555E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.412413749E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(272.5637141, aProperties.m_SpecificHeat, 0.001);
    EXPECT_NEAR(3.225849103, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(1.26127756E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.592921334, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesQuadrupleGas, TestRealPropertiesLowPressure)
{
    SCOPED_TRACE("Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], "
                 "Pressure = 90,000 [Pa]");

    m_Gas.setTemperatureAndPressure(300, 90000);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(79.4114, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.108977555E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.412413749E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(272.5637141, aProperties.m_SpecificHeat, 0.001);
    EXPECT_NEAR(2.865298981, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(1.41998832E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.592921334, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesQuadrupleGas, TestRealPropertiesLowPressureRepeat)
{
    SCOPED_TRACE("Begin Test: Gas Properties (quadruple gas) real mix - Temperature = 300 [K], "
                 "Pressure = 90,000 [Pa] (Repeatability)");

    m_Gas.setTemperatureAndPressure(300, 90000);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(79.4114, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.108977555E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.412413749E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(272.5637141, aProperties.m_SpecificHeat, 0.001);
    EXPECT_NEAR(2.865298981, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(1.41998832E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.592921334, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesQuadrupleGas, TotalPercents)
{
    SCOPED_TRACE("Begin Test: Gas Properties (quadruple gas) - Total percents.");

    double percents = m_Gas.totalPercent();

    EXPECT_NEAR(1.0, percents, 1e-6);
}
