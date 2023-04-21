#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

class TestSingleClearSolarCond001 : public testing::Test
{
private:
    std::unique_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 300.0;   // Kelvins
        auto airSpeed = 5.5;           // meters per second
        auto tSky = 270.0;             // Kelvins
        auto solarRadiation = 1000.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 294.15;

        auto aIndoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(aIndoor != nullptr);

        /////////////////////////////////////////////////////////
        // IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 100.0;
        auto solarAbsorptance = 0.094189159572;

        auto aSolidLayer =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        aSolidLayer->setSolarAbsorptance(solarAbsorptance, solarRadiation);
        ASSERT_TRUE(aSolidLayer != nullptr);

        double windowWidth = 1;
        double windowHeight = 1;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayer(aSolidLayer);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::unique_ptr<Tarcog::ISO15099::CSingleSystem>(
          new Tarcog::ISO15099::CSingleSystem(aIGU, aIndoor, Outdoor));
        ASSERT_TRUE(m_TarcogSystem != nullptr);

        m_TarcogSystem->solve();
    }

public:
    Tarcog::ISO15099::CSingleSystem * GetSystem() const
    {
        return m_TarcogSystem.get();
    };
};

TEST_F(TestSingleClearSolarCond001, TestTempAndRad)
{
    SCOPED_TRACE("Begin Test: Single Clear (Solar Radiation) - Temperatures and Radiosity.");

    auto aSystem = GetSystem();
    ASSERT_TRUE(aSystem != nullptr);

    const auto Temperature = aSystem->getTemperatures();
    std::vector<double> correctTemperature = {299.627742, 299.627975};
    ASSERT_EQ(correctTemperature.size(), Temperature.size());

    for(auto i = 0u; i < correctTemperature.size(); ++i)
    {
        EXPECT_NEAR(correctTemperature[i], Temperature[i], 1e-5);
    }

    const auto Radiosity = aSystem->getRadiosities();
    std::vector<double> correctRadiosity = {444.699763, 451.769813};
    ASSERT_EQ(correctRadiosity.size(), Radiosity.size());

    for(auto i = 0u; i < correctRadiosity.size(); ++i)
    {
        EXPECT_NEAR(correctRadiosity[i], Radiosity[i], 1e-5);
    }
}

TEST_F(TestSingleClearSolarCond001, TestIndoor)
{
    SCOPED_TRACE("Begin Test: Single Clear (Solar Radiation) - Indoor heat flow.");

    auto aSystem = GetSystem();

    auto convectiveHF = aSystem->getConvectiveHeatFlow(Tarcog::ISO15099::Environment::Indoor);
    auto radiativeHF = aSystem->getRadiationHeatFlow(Tarcog::ISO15099::Environment::Indoor);
    auto totalHF = aSystem->getHeatFlow(Tarcog::ISO15099::Environment::Indoor);

    EXPECT_NEAR(-12.135453, convectiveHF, 1e-5);
    EXPECT_NEAR(-27.311063, radiativeHF, 1e-5);
    EXPECT_NEAR(-39.446516, totalHF, 1e-5);
}
