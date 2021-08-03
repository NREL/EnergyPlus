#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

class TestIndoorEnvironment : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CEnvironment> m_Indoor;
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        // Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 300.0;   // Kelvins
        auto airSpeed = 5.5;           // meters per second
        auto tSky = 270.0;             // Kelvins
        auto solarRadiation = 0.0;

        auto Outdoor = Tarcog::ISO15099::Environments::outdoor(
          airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        ASSERT_TRUE(Outdoor != nullptr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);

        /////////////////////////////////////////////////////////
        // Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 294.15;

        m_Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(m_Indoor != nullptr);

        /////////////////////////////////////////////////////////
        // IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 100.0;

        auto solidLayer =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(solidLayer != nullptr);

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayer(solidLayer);

        /////////////////////////////////////////////////////////
        // System
        /////////////////////////////////////////////////////////
        m_TarcogSystem = std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, m_Indoor, Outdoor);
        m_TarcogSystem->solve();
        ASSERT_TRUE(m_TarcogSystem != nullptr);
    }

public:
    std::shared_ptr<Tarcog::ISO15099::CEnvironment> GetIndoors() const
    {
        return m_Indoor;
    };
};

TEST_F(TestIndoorEnvironment, IndoorRadiosity)
{
    SCOPED_TRACE("Begin Test: Indoors -> Radiosity");

    auto aIndoor = GetIndoors();
    ASSERT_TRUE(aIndoor != nullptr);

    auto radiosity = aIndoor->getEnvironmentIR();
    EXPECT_NEAR(424.458749869075, radiosity, 1e-6);
}

TEST_F(TestIndoorEnvironment, IndoorConvection)
{
    SCOPED_TRACE("Begin Test: Indoors -> Convection Flow");

    auto aIndoor = GetIndoors();
    ASSERT_TRUE(aIndoor != nullptr);

    auto convectionFlow = aIndoor->getConvectionConductionFlow();
    EXPECT_NEAR(-5.826845, convectionFlow, 1e-6);
}

TEST_F(TestIndoorEnvironment, IndoorHc)
{
    SCOPED_TRACE("Begin Test: Indoors -> Convection Coefficient");

    auto aIndoor = GetIndoors();
    ASSERT_TRUE(aIndoor != nullptr);

    auto hc = aIndoor->getHc();
    EXPECT_NEAR(1.913874, hc, 1e-6);
}
