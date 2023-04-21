#include <memory>
#include <string>
#include <gtest/gtest.h>

#include "WCEGases.hpp"

using namespace Gases;

class TestGasPropertiesDoubleGas : public testing::Test
{
protected:
    CGas m_Gas;

    virtual void SetUp()
    {
        const CGasData Air{"Air",
                           28.97,                             // Molecular weight
                           1.4,                               // Specific heat ratio
                           {1.002737e+03, 1.2324e-02, 0.0},   // Specific heat coefficients
                           {2.8733e-03, 7.76e-05, 0.0},       // Conductivity coefficients
                           {3.7233e-06, 4.94e-08, 0.0}};      // Viscosity coefficients

        const CGasData Argon{"Argon",
                             39.948,                           // Molecular weight
                             1.67,                             // Specific heat ratio
                             {5.21929e+02, 0.0, 0.0},          // Specific heat coefficients
                             {2.2848e-03, 5.1486e-05, 0.0},    // Conductivity coefficients
                             {3.3786e-06, 6.4514e-08, 0.0}};   // Viscosity coefficients

        m_Gas.addGasItems({{0.1, Air}, {0.9, Argon}});

        // This is alternative way of adding mixtures
        // m_Gas.addGasItem(0.1, Air);
        // m_Gas.addGasItem(0.9, Argon);
    }
};

TEST_F(TestGasPropertiesDoubleGas, TestSimpleProperties)
{
    SCOPED_TRACE("Begin Test: Gas Properties (Air 10% / Argon 90%) simple mix - Temperature = 300 "
                 "[K], Pressure = 101325 [Pa]");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getSimpleGasProperties();

    EXPECT_NEAR(38.8502, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.85728700E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.23138500E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(570.37952, aProperties.m_SpecificHeat, 0.0001);
    EXPECT_NEAR(1.578172439, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(2.06329175E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.685266362, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesDoubleGas, TestSimplePropertiesRepeat)
{
    SCOPED_TRACE("Begin Test: Gas Properties (Air 10% / Argon 90%) simple mix - Temperature = 300 "
                 "[K], Pressure = 101325 [Pa] (Repeatability)");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getSimpleGasProperties();

    EXPECT_NEAR(38.8502, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.85728700E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.23138500E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(570.37952, aProperties.m_SpecificHeat, 0.0001);
    EXPECT_NEAR(1.578172439, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(2.06329175E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.685266362, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesDoubleGas, TestRealProperties)
{
    SCOPED_TRACE("Begin Test: Gas Properties (Air 10% / Argon 90%) real mix - Temperature = 300 "
                 "[K], Pressure = 101325 [Pa]");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(38.8502, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.850941662E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.235785737E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(558.0578118, aProperties.m_SpecificHeat, 0.0001);
    EXPECT_NEAR(1.578172439, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(2.10164367E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.674088072, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesDoubleGas, TestRealPropertiesRepeat)
{
    SCOPED_TRACE("Begin Test: Gas Properties (Air 10% / Argon 90%) real mix - Temperature = 300 "
                 "[K], Pressure = 101325 [Pa] (Repeatability)");

    m_Gas.setTemperatureAndPressure(300, 101325);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(38.8502, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.850941662E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.235785737E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(558.0578118, aProperties.m_SpecificHeat, 0.0001);
    EXPECT_NEAR(1.578172439, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(2.10164367E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.674088072, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesDoubleGas, TestRealPropertiesLowPressure)
{
    SCOPED_TRACE("Begin Test: Gas Properties (Air 10% / Argon 90%) real mix - Temperature = 300 "
                 "[K], Pressure = 90,000 [Pa]");

    m_Gas.setTemperatureAndPressure(300, 90000);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(38.8502, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.850941662E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.235785737E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(558.0578118, aProperties.m_SpecificHeat, 0.0001);
    EXPECT_NEAR(1.401781589, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(2.36610050E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.674088072, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesDoubleGas, TestRealPropertiesLowPressureRepeat)
{
    SCOPED_TRACE("Begin Test: Gas Properties (Air 10% / Argon 90%) real mix - Temperature = 300 "
                 "[K], Pressure = 90,000 [Pa] (Repeatability)");

    m_Gas.setTemperatureAndPressure(300, 90000);
    auto aProperties = m_Gas.getGasProperties();

    EXPECT_NEAR(38.8502, aProperties.m_MolecularWeight, 0.0001);
    EXPECT_NEAR(1.850941662E-02, aProperties.m_ThermalConductivity, 1e-6);
    EXPECT_NEAR(2.235785737E-05, aProperties.m_Viscosity, 1e-6);
    EXPECT_NEAR(558.0578118, aProperties.m_SpecificHeat, 0.0001);
    EXPECT_NEAR(1.401781589, aProperties.m_Density, 0.0001);
    EXPECT_NEAR(2.36610050E-05, aProperties.m_Alpha, 1e-6);
    EXPECT_NEAR(0.674088072, aProperties.m_PrandlNumber, 0.0001);
}

TEST_F(TestGasPropertiesDoubleGas, TotalPercents)
{
    SCOPED_TRACE("Begin Test: Gas Properties (Air 10% / Argon 90%) - Total percents.");

    double percents = m_Gas.totalPercent();

    EXPECT_NEAR(1.0, percents, 1e-6);
}
