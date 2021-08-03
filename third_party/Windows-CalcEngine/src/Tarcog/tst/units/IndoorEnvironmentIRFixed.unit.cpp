#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"

class TestIndoorEnvironmentIRFixed : public testing::Test
{
private:
    std::shared_ptr<Tarcog::ISO15099::CEnvironment> m_Indoor;
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> m_TarcogSystem;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
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
        /// Indoor
        /////////////////////////////////////////////////////////

        auto roomTemperature = 294.15;
        // I just have picked up solution from WINDOW 7.4 run. There is no other way of
        // getting the results since current version of WINDOW does not support IR input.
        auto IRRadiation = 424.458750;

        m_Indoor = Tarcog::ISO15099::Environments::indoor(roomTemperature);
        ASSERT_TRUE(m_Indoor != nullptr);
        m_Indoor->setEnvironmentIR(IRRadiation);

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        auto solidLayerThickness = 0.003048;   // [m]
        auto solidLayerConductance = 100.0;

        auto aSolidLayer =
          Tarcog::ISO15099::Layers::solid(solidLayerThickness, solidLayerConductance);
        ASSERT_TRUE(aSolidLayer != nullptr);

        auto windowWidth = 1.0;
        auto windowHeight = 1.0;
        Tarcog::ISO15099::CIGU aIGU(windowWidth, windowHeight);
        aIGU.addLayer(aSolidLayer);

        /////////////////////////////////////////////////////////
        /// System
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

TEST_F(TestIndoorEnvironmentIRFixed, IndoorRadiosity)
{
    SCOPED_TRACE("Begin Test: Indoors -> Fixed radiosity (user input).");

    auto aIndoor = GetIndoors();
    ASSERT_TRUE(aIndoor != nullptr);

    auto radiosity = aIndoor->getEnvironmentIR();
    EXPECT_NEAR(424.458750, radiosity, 1e-6);
}

TEST_F(TestIndoorEnvironmentIRFixed, IndoorConvection)
{
    SCOPED_TRACE("Begin Test: Indoors -> Convection Flow (user input).");

    auto aIndoor = GetIndoors();
    ASSERT_TRUE(aIndoor != nullptr);

    auto convectionFlow = aIndoor->getConvectionConductionFlow();
    EXPECT_NEAR(-5.826845, convectionFlow, 1e-6);
}

TEST_F(TestIndoorEnvironmentIRFixed, IndoorHc)
{
    SCOPED_TRACE("Begin Test: Indoors -> Convection Coefficient (user input).");

    auto aIndoor = GetIndoors();
    ASSERT_TRUE(aIndoor != nullptr);

    auto hc = aIndoor->getHc();
    EXPECT_NEAR(1.913874, hc, 1e-6);
}
